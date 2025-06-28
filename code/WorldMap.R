# ============================================================
# Title:      Map with Coefficients on Border Triangles
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script print a colored dot on every
#             border triangle that represents the value
#             of the coefficient under consideration
# ============================================================
rm(list = ls())

library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(patchwork)
library(sf)
library(latex2exp)
library(here)

size <- 7.5

# Get world map data and position points on border triangles (countries vs religions) --------------
world <- ne_countries(scale = 10, returnclass = "sf")
  
points_data <- data.frame(
  lon = c(-67.2, -62.6, -54.5, -57.5, -58.1, -69.6, -69.5, -75.3, -70.1, -67, 
          -60.7, -56.8, -89.1, -89.1, 20, 25.2, 32, 31.3, 29.2, 23.2, 24, 30.3, 
          33, 34.8, 33.1, 30.6, 33.9, 30.5, 30.5, 29.1, 29.5, 29.4, 41.9, 35.3, 
          33.9, 30.9, 27.3, 24, 34.1, 18.5, 16.2, 13.2, 11.3, 22.9, 24, 25, 36.5, 
          42.4, 42.8, 15, 11.9, 9.6, -8.7, -5, 4.2, 13.5, 14.2, 15.5, 3.5, -2.8, 
          0, 52, 46.8, 39.2, 38.8, 35.8, 35.8, 42.2, 44.7, 61.1, 77.3, 80.9, 91.7, 
          92.7, 97.3, 100.9, 100.1, 102, 107.3, 105.5, 116.5, 130.5, 86.9, 80.1, 
          73.6, 74.8, 74.5, 70.7, 70.9, 67.8, 66.5, 56, 61.3, 46.2, 43.6, 45, 
          46.5, 31.7, 26.6, 27.7, 28.2, 29.2, 20.8, 23.6, 23.7, 15, 18.7, 22.6, 
          26.8, 22.7, 20.5, 22.8, 22, 26.4, 23, 22.4, 21, 20.5, 20, 20.5, 21.6, 
          19.4, 19.2, 19, 16.5, 16.3, 17, 16.9, 13.5, 9.5, 10.3, 7, 7.6, 6.6, 
          5.6, 6.1, 6, 13.6),
  lat = c(-22.9, -22, -25.5, -30.1, -19.8, -17.7, -11, -0.2, -4, 1.5, 
          5.2, 2, 17.8, 14.5, -25, -17.8, -26, -22.5, -22, -17.5, -11.1, -15.6, 
          -14, -11.5, -9.6, -8.2, -1, -1, -2.5, -2.8, -1.5, -4.5, 4, 5.5, 4, 3.7, 
          5.2, 8.6, 9.7, 3.5, 2.3, 2.1, 2.2, 11, 19.5, 22, 14.5, 12.5, 11, 22.9, 
          23.5, 30.3, 27.2, 25, 19, 13.8, 13, 7.5, 11.9, 9.7, 11, 19, 29.2, 32, 
          33.3, 32.5, 33.5, 37, 37.1, 29.7, 35.4, 30.1, 27.8, 22, 28.3, 21.5, 
          20.5, 22.5, 14.3, 14.3, 49.8, 42.8, 49.2, 42.3, 39.4, 37.5, 36.8, 40.3, 
          42.2, 37.2, 37.4, 41.4, 35.5, 41.9, 41, 41.2, 39.1, 52.2, 55.6, 57.5, 
          56.2, 69.3, 69.1, 53.9, 51.5, 51, 49.5, 49, 48.3, 44, 46.1, 48, 48.5, 
          41.6, 41.4, 42.3, 40.8, 42, 42.5, 43, 42.4, 43.5, 45, 45.9, 46.3, 47, 
          48, 48.8, 48.7, 47.5, 46.7, 46, 47.6, 49.5, 49.5, 50.1, 50.9, 46.4),
  label = c("ChBoAr", "BoArPa", "BrArPa", "BrArUr", "BrBoPa", "ChBoPe", "BrBoPe", "EcCoPe", "BrCoPe", 
            "BrCoVe", "BrVeGu", "BrGuSu", "BeGuMe", "HoGuEl", "SoNaBo", "ZaNaBoZi", "SoEsMo", "SoZiMo", 
            "SoZiBo", "ZaAnNa", "AnCoZa", "ZaMoZi", "ZaMoMa", "TaMoMa", "TaZaMa", "TaCoZa", "KeUgTa", 
            "RuUgTa", "RuBuTa", "CoBuRw", "CoUgRu", "CoBuTa", "KeEtSo", "SsEtKe", "SsUgKe", "SsUgCo", 
            "SsCaCo", "SuCaSs", "SsEtSu", "CoCoCa", "CoCaCa", "CoCaGa", "EgCaGa", "SuCaCh", "LiChSu", 
            "LiEgSu", "SuEtEr", "DsEtEr", "DsEtSo", "LiNiCh", "LiNiAl", "TuLiAl", "MoMaAl", "MaMaAl", 
            "MaNiAl", "NiNiCh", "CaNiCh", "CaCaCh", "NiNiBe", "CiGhBf", "ToGhBf", "JeOmSa", "IrKuSa", 
            "IrJoSa", "IrJoSy", "IsSyJo", "IsSyLe", "IrSyTu", "IrIrTu", "AfIrPa", "PaChIn", "NeChIn", 
            "BhChIn", "BaMyIn", "MyChIn", "LaMyCh", "LaMyTh", "LaViCh", "CaLaVi", "CaLaTh", "MoRuCh", 
            "NkRuCh", "KaChRu", "KaChKi", "KyChTa", "AfChTa", "AfChPa", "KiTaUz", "KaUzKi", "AfUzTa", 
            "AfUzTu", "KaTuUz", "AfIrTu", "RuGeAz", "ArGeTu", "ArAsGe", "ArAsIr", "BeUkRu", "LaBeLi", 
            "LaEsRu", "LaBeRu", "FiNoRu", "FiNoSw", "PoBeLi", "PoBeUk", "PoGeCz", "PoSlCz", "PoSlUk", 
            "RoUkMo", "BuRoSe", "HuRoSe", "RoUkHu", "SlUkHu", "BuTuGr", "BuNmGr", "BuNmSe", "AlNmGr", 
            "AlNmKo", "AlMoKo", "SeMoKo", "SeNmKo", "BoMoSe", "BoCrSe", "HuCrSe", "HuCrSl", "HuAuSl", 
            "HuSlAu", "CzSlAu", "AuGeCz", "AuGeSw", "AuSwIt", "FrSwIt", "FrSwGe", "LuFrGe", "LuFrBe", 
            "LuGeBe", "NeBeGe", "AuSlIt"),
  cramerv = c(0.07108152, 0.06380237, 0.09073317, 0.09157442, 0.00593042, 0.02359642, 0.01048599, 0.009934964, 0.0108985, 
              0.0169125, 0.09993574, 0.1985194, 0.01068791, 0.01930349, 0.02385454, 0.01764561, 0.2519945, 0.2590275,
              0.03044927, 0.01194562, 0.01098114, 0.2561863, 0.1868204, 0.1412728, 0.2392399, 0.3514438, 0.2112253,
              0.2209919, 0.2499502, 0.05468285, 0.1633864, 0.3475559, 0.3475243, 0.189865, 0.02387771, 0.1656478,
              0.152353, 0.5794066, 0.3853567, 0.1492207, 0.1724263, 0.1827813, 0.1134877, 0.4178357, 0.2887045,
              0.06562038, 0.3684267, 0.09775539, 0.3093684, 0.3528634, 0.009375468, 0.01381256, 0.01712339, 0.07274984,
              0.07302156, 0.217089, 0.1112698, 0.2857718, 0.2203855, 0.2933233, 0.3377083, 0.1187267, 0.1469731,
              0.1165814, 0.08535811, 0.5914282, 0.6628411, 0.09518755, 0.01868914, 0.04414479, 0.4497253, 0.3608089,
              0.3619365, 0.2580035, 0.361211, 0.04343062, 0.3626358, 0.08015971, 0.3188073, 0.2991536, 0.09353833,
              0.08761395, 0.2609957, 0.3320383, 0.3722167, 0.5071439, 0.5825267, 0.0695043, 0.2814539, 0.05387009,
              0.06865213, 0.2867311, 0.05236857, 0.4069833, 0.6671706, 0.6477722, 0.612358, 0.1038069, 0.02581464,
              0.03100333, 0.06111145, 0.03877543, 0.101006, 0.02393402, 0.06430619, 0.1337699, 0.02108973, 0.06603959,
              0.04367463, 0.1766898, 0.1517594, 0.05429286, 0.04980211, 0.6624045, 0.1854796, 0.1758968, 0.4111485,
              0.331021, 0.3450268, 0.5477222, 0.4982178, 0.3216566, 0.3523457, 0.1303253, 0.09024269, 0.1483112,
              0.1718938, 0.1589938, 0.04795946, 0.008496698, 0.04040116, 0.09770329, 0.06082414, 0.06110729, 0.03162206,
              0.01933028, 0.02526603, 0.03679857),
  contcoef = c(0.1000204, 0.0898651, 0.1272726, 0.1284332, 0.008386586, 0.03335181, 0.0148278, 0.01404877, 0.01541097,
               0.02391104, 0.1399398, 0.2702984, 0.01511326, 0.02728909, 0.03371624, 0.02494689, 0.335694, 0.3439678,
               0.0430219, 0.01689124, 0.01552781, 0.3406349, 0.255439, 0.1959181, 0.3204897, 0.445075, 0.2862205,
               0.2983009, 0.3332744, 0.07710302, 0.2251315, 0.4411137, 0.4410813, 0.259324, 0.03374895, 0.2280864,
               0.2106262, 0.6338042, 0.4785287, 0.2064823, 0.2369059, 0.2502658, 0.1584679, 0.5087292, 0.3779975,
               0.09240419, 0.4620744, 0.1369445, 0.4008289, 0.4465149, 0.01325775, 0.01953019, 0.02420903, 0.1023436,
               0.1027218, 0.2934902, 0.1554465, 0.3746991, 0.2975549, 0.3831629, 0.4309639, 0.165587, 0.203502,
               0.1626749, 0.1198446, 0.6415742, 0.6839019, 0.1334121, 0.02642121, 0.06230886, 0.5366617, 0.4545104,
               0.4556363, 0.3427682, 0.4549122, 0.06130465, 0.4563335, 0.1126415, 0.4110177, 0.3896323, 0.1311407,
               0.1229645, 0.3462691, 0.4250446, 0.4658005, 0.5828104, 0.6358396, 0.0978225, 0.369817, 0.07596368,
               0.09663439, 0.3757797, 0.07385806, 0.4988366, 0.6862687, 0.6754928, 0.6546449, 0.1452483, 0.03648311,
               0.04380324, 0.08610367, 0.05475448, 0.1414087, 0.03382844, 0.09056893, 0.1858822, 0.02981212, 0.09298941,
               0.06164778, 0.2424234, 0.2098418, 0.07655637, 0.07025678, 0.683662, 0.2537241, 0.241399, 0.5026569,
               0.4239767, 0.4385229, 0.6123722, 0.5759759, 0.4140637, 0.4459902, 0.181255, 0.1265956, 0.205277,
               0.2362151, 0.219374, 0.06766946, 0.01201528, 0.05704284, 0.1368729, 0.08570184, 0.08609786, 0.0446757,
               0.02732694, 0.03570877, 0.05197071),
  GKtau = c(0.006544888, 0.005711228, 0.01381718, 0.01546116, 5.309463e-05, 0.0006175755, 0.0001712962, 0.0001242691, 0.0001515269,
            0.0001873878, 0.001040657, 0.05451455, 0.0001088612, 0.0004325693, 0.0008405988, 0.0001684446, 0.1186248, 0.07597919,
            0.001516449, 6.547921e-05, 0.00015745, 0.07521355, 0.03505031, 0.02350116, 0.07036364, 0.1698684, 0.04749935,
            0.06212857, 0.08404296, 0.00341175, 0.04055167, 0.1829812, 0.06911221, 0.05977555, 0.0006104434, 0.04668628,
            0.03071673, 0.4818024, 0.2136019, 0.01960405, 0.03654624, 0.05035028, 0.01797963, 0.1758508, 0.100617,
            0.005847071, 0.2497534, 0.009813188, 0.1789533, 0.1543103, 4.048537e-05, 0.0001509629, 0.0005015672, 0.007921081,
            0.004579089, 0.06397265, 0.008360761, 0.1002777, 0.06162344, 0.09196702, 0.1569479, 0.02377955, 0.02262725,
            0.01907033, 0.009452734, 0.2944876, 0.4639545, 0.006211553, 0.0004287706, 0.002439188, 0.1667623, 0.2549966,
            0.2619232, 0.0977221, 0.2463292, 0.00326865, 0.2321637, 0.01258103, 0.154569, 0.1182479, 0.01522363,
            0.01528578, 0.0301722, 0.1608366, 0.206571, 0.4111258, 0.4702171, 0.003164383, 0.1121781, 0.00429082,
            0.004971597, 0.1136022, 0.001568579, 0.239268, 0.65486, 0.5438039, 0.1631516, 0.01729935, 0.0005418758,
            0.00156619, 0.006237598, 0.002135477, 0.01107905, 0.0008711315, 0.006260771, 0.03124714, 0.0003958002, 0.00704026,
            0.003171875, 0.03701014, 0.01992498, 0.003313298, 0.003243143, 0.6388369, 0.02518227, 0.02053356, 0.2210288,
            0.09696914, 0.08315006, 0.4348757, 0.2651431, 0.1713263, 0.1026227, 0.02045005, 0.008505104, 0.03300992,
            0.03137824, 0.02931318, 0.001444883, 4.319832e-05, 0.002168982, 0.01571566, 0.005805636, 0.007083558, 0.001378996,
            0.0003927391, 0.000878257, 0.001818842),
  GKlambda = c(0, 0, 0.01711374, 0.01868581, 0, 0.001158117, 0, 2.042118e-06, 0,
               0, 0, 0, 0, 0.0008477922, 0, 2.374671e-05, 0.1459142, 0.1050648,
               0, 5.172635e-06, 0, 6.28174e-06, 3.026848e-06, 2.883491e-07, 3.463979e-07, 0.204556, 0.06274672,
               0.04205784, 0, 0, 0.06668814, 0.2156904, 0.1318504, 0, 0.004807723, 0.07315183,
               0, 0.5538243, 0.2750004, 0, 0, 0, 0, 0.2064344, 0.1109512,
               3.2735e-07, 0.2462317, 0.02902118, 0.2325649, 0.1902532, 2.453832e-06, 0.000102635, 5.367628e-05, 0.01416976,
               0.007969889, 0.007617385, 0.03223519, 0.2481111, 0.02181819, 0.2041356, 0.3091033, 1.682459e-06, 0.04737382,
               0.04138939, 0.01877554, 0.3950279, 0.5091708, 0.008628771, 0.0001115216, 5.723799e-05, 0.2795852, 0.3836609,
               0.3881754, 0.01035134, 0.3824414, 0, 0.4372856, 0, 0.05314241, 0.1295398, 0.06474721,
               0.06422365, 0.09429408, 0.195312, 0.2567476, 0.4920069, 0.6426304, 0, 0.1527477, 0.00955329,
               0.01037606, 0.1530217, 0, 0.2436398, 0.676854, 0.677985, 0.2880976, 0, 0,
               0, 0, 0, 0, 0.0006014771, 0, 0, 7.159443e-05, 0,
               0, 0.05271197, 0.02897984, 3.827871e-05, 7.84222e-05, 0.7095574, 0.02525299, 0.03461634, 0.2748769,
               0.1761898, 0.1003336, 0.5582639, 0.3845142, 0.168928, 0.1090406, 0.0412478, 0.00828176, 0.06361609,
               0.04948332, 0, 0, 0, 0, 0.0596119, 0.01769693, 0.01977885, 0,
               0, 0, 0),
  UncertCoef = c(0.01346343, 0.01402566, 0.01837438, 0.02084166, 0.000292512, 0.001023019, 0.0005836464, 0.0002358791, 0.0004138041,
                 0.0005922764, 0.005595646, 0.1231787, 0.000580838, 0.0006840075, 0.003885619, 0.0006126376, 0.119256, 0.09594978,
                 0.00325657, 0.0004000875, 0.0002662359, 0.1054247, 0.06064928, 0.02658013, 0.09730463, 0.1880235, 0.05391693,
                 0.06921891, 0.111925, 0.005960545, 0.04602758, 0.1913067, 0.1845038, 0.05930076, 0.0009168187, 0.05233173,
                 0.04890405, 0.513977, 0.2440457, 0.04236861, 0.06363469, 0.07811985, 0.02996327, 0.23759, 0.1195354,
                 0.01072378, 0.2335325, 0.02472354, 0.2160404, 0.194675, 0.0001601229, 0.0004025296, 0.0007365928, 0.01063962,
                 0.007778945, 0.1035594, 0.02065045, 0.1021044, 0.1123899, 0.1019382, 0.1436132, 0.03775804, 0.04167183,
                 0.02600075, 0.01285121, 0.4010936, 0.4653284, 0.01172285, 0.0006731945, 0.005650111, 0.2616956, 0.19935,
                 0.2056646, 0.1420633, 0.1944088, 0.004993176, 0.1934409, 0.02549725, 0.1789855, 0.1507457, 0.01498725,
                 0.01381666, 0.07793899, 0.1838828, 0.2694608, 0.4439521, 0.4894227, 0.007680969, 0.1243433, 0.007277241,
                 0.01105206, 0.1281485, 0.006271875, 0.2946125, 0.7769665, 0.6375703, 0.3455404, 0.02998236, 0.001172848,
                 0.006948461, 0.02055494, 0.006131902, 0.01587602, 0.001355399, 0.00965159, 0.05066534, 0.00110994, 0.01120586,
                 0.005134611, 0.05554199, 0.03173494, 0.00599469, 0.007634101, 0.7300581, 0.0432975, 0.03772781, 0.2270293,
                 0.1397822, 0.1602682, 0.4387559, 0.3305601, 0.1492885, 0.1475581, 0.02993183, 0.01492176, 0.04427444,
                 0.04996083, 0.04995671, 0.009497587, 0.0001626768, 0.003157655, 0.01621297, 0.005928165, 0.007002734, 0.002717926,
                 0.001116434, 0.001177904, 0.003299415)
)

CramerV_World <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = cramerv), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) 
ggsave(plot = CramerV_World, filename = here("results/Plots/CramerV_World.pdf"), height = 200, width = (1+sqrt(5))/2*200, device = "pdf", units = "mm")

contcoef_World <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = contcoef), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) 
ggsave(plot = contcoef_World, filename = here("results/Plots/contcoef_World.pdf"), height = 200, width = (1+sqrt(5))/2*200, device = "pdf", units = "mm")

GKtau_World <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = GKtau), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) 
ggsave(plot = GKtau_World, filename = here("results/Plots/GKtau_World.pdf"), height = 200, width = (1+sqrt(5))/2*200, device = "pdf", units = "mm")

GKlambda_World <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = GKlambda), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) 
ggsave(plot = GKlambda_World, filename = here("results/Plots/GKlambda_World.pdf"), height = 200, width = (1+sqrt(5))/2*200, device = "pdf", units = "mm")

UncertCoef_World <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = UncertCoef), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) 
ggsave(plot = UncertCoef_World, filename = here("results/Plots/UncertCoef_World.pdf"), height = 200, width = (1+sqrt(5))/2*200, device = "pdf", units = "mm")

CramerV_Europe <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = cramerv), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) +
  coord_sf(xlim = c(5, 32.3), ylim = c(40.5, 57.8), expand = FALSE)
ggsave(plot = CramerV_Europe, filename = here("results/Plots/CramerV_Europe.pdf"), height = 200, width = 221, device = "pdf", units = "mm")

contcoef_Europe <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = contcoef), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) +
  coord_sf(xlim = c(5, 32.3), ylim = c(40.5, 57.8), expand = FALSE)
ggsave(plot = contcoef_Europe, filename = here("results/Plots/contcoef_Europe.pdf"), height = 200, width = 221, device = "pdf", units = "mm")

GKtau_Europe <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = GKtau), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) +
  coord_sf(xlim = c(5, 32.3), ylim = c(40.5, 57.8), expand = FALSE)
ggsave(plot = GKtau_Europe, filename = here("results/Plots/GKtau_Europe.pdf"), height = 200, width = 221, device = "pdf", units = "mm")

GKlambda_Europe <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = GKlambda), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) +
  coord_sf(xlim = c(5, 32.3), ylim = c(40.5, 57.8), expand = FALSE)
ggsave(plot = GKtau_Europe, filename = here("results/Plots/GKlambda_Europe.pdf"), height = 200, width = 221, device = "pdf", units = "mm")

UncertCoef_Europe <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data, aes(x = lon, y = lat, color = UncertCoef), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) +
  coord_sf(xlim = c(5, 32.3), ylim = c(40.5, 57.8), expand = FALSE)
ggsave(plot = UncertCoef_Europe, filename = here("results/Plots/UncertCoef_Europe.pdf"), height = 200, width = 221, device = "pdf", units = "mm")

points_data_Europe <- data.frame(
  lon = c(31.7, 26.6, 27.7, 28.2, 23.6, 23.7, 15, 18.7, 22.6, 
          26.8, 22.7, 20.5, 22.8, 22, 26.4, 23, 22.4, 21, 20.5, 20, 20.5, 21.6, 
          19.4, 19.2, 19, 16.5, 16.3, 17, 16.9, 13.5, 9.5, 10.3, 7, 7.6, 6.6, 
          5.6, 6.1, 6, 13.6),
  lat = c(52.2, 55.6, 57.5, 
          56.2, 53.9, 51.5, 51, 49.5, 49, 48.3, 44, 46.1, 48, 48.5, 
          41.6, 41.4, 42.3, 40.8, 42, 42.5, 43, 42.4, 43.5, 45, 45.9, 46.3, 47, 
          48, 48.8, 48.7, 47.5, 46.7, 46, 47.6, 49.5, 49.5, 50.1, 50.9, 46.4),
  label = c("BeUkRu", "LaBeLi", "LaEsRu", "LaBeRu", "PoBeLi", "PoBeUk", "PoGeCz", "PoSlCz", "PoSlUk", 
            "RoUkMo", "BuRoSe", "HuRoSe", "RoUkHu", "SlUkHu", "BuTuGr", "BuNmGr", "BuNmSe", "AlNmGr", 
            "AlNmKo", "AlMoKo", "SeMoKo", "SeNmKo", "BoMoSe", "BoCrSe", "HuCrSe", "HuCrSl", "HuAuSl", 
            "HuSlAu", "CzSlAu", "AuGeCz", "AuGeSw", "AuSwIt", "FrSwIt", "FrSwGe", "LuFrGe", "LuFrBe", 
            "LuGeBe", "NeBeGe", "AuSlIt"),
  wermuthcoef = c(0.7594435, 0.3331149, 0.9226419, 0.9321676, 0.5428084, 0.8299367, 0.969063, 0.5290542, 0.8903236,
                  0.6102135, 0.779378, 0.8346488, 0.5800878, 0.7072875, 0.9936817, 0.5419116, 0.4929697, 0.8577225,
                  0.736384, 0.8151939, 0.946691, 0.9069089, 0.7954003, 0.8432995, 0.7907629, 0.7157109, 0.8027942,
                  0.899647, 0.9511036, 0.2546134, 0.04737463, 0.2184952, 0.4115908, 0.229921, 0.2486963, 0.1698809,
                  0.1190534, 0.1230555, 0.2494335)
)

wermuthcoef_Europe <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data_Europe, aes(x = lon, y = lat, color = wermuthcoef), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank()
  ) + 
  coord_sf(xlim = c(5, 32.3), ylim = c(40.5, 57.8), expand = FALSE)
ggsave(plot = wermuthcoef_Europe, filename = here("results/Plots/wermuthcoef_Europe.pdf"), height = 200, width = 221, device = "pdf", units = "mm")

CramerV_Wermuth_Europe <- (CramerV_Europe + theme(plot.margin = unit(c(0,0,10,0), "mm"), legend.key.size = unit(1.2, "cm"), legend.text = element_text(size = 15))) / wermuthcoef_Europe + plot_layout(guides = 'collect') + theme(legend.key.size = unit(1.2, "cm"), legend.text = element_text(size = 15))
ggsave(plot = CramerV_Wermuth_Europe, filename = here("results/Plots/CramerV_Wermuth_Europe.pdf"), height = 410, width = 240, device = "pdf", units = "mm")
separator <- ggplot() + 
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
CramerV_Wermuth_Europe <- (CramerV_Europe + theme(plot.margin = unit(c(0,10,0,0), "mm"), legend.key.size = unit(1.2, "cm"), legend.text = element_text(size = 20))) + separator + (wermuthcoef_Europe + theme(plot.margin = unit(c(0,0,0,10), "mm"), legend.key.size = unit(1.2, "cm"), legend.text = element_text(size = 20)))  + plot_layout(guides = 'collect', widths = c(1, 0.01, 1))
ggsave(plot = CramerV_Wermuth_Europe, filename = here("results/Plots/CramerV_Wermuth_Europe_wide.pdf"), height = 220, width = 430, device = "pdf", units = "mm")

black_line <- ggplot() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))

# Define layout areas (3x3 grid)
P1 <- area(t = 1, l = 1, b = 1, r = 1)
V1 <- area(t = 1, l = 2, b = 1, r = 2)
P2 <- area(t = 1, l = 3, b = 1, r = 3)

H1 <- area(t = 2, l = 1, b = 2, r = 1)
X  <- area(t = 2, l = 2, b = 2, r = 2)
H2 <- area(t = 2, l = 3, b = 2, r = 3)

P3 <- area(t = 3, l = 1, b = 3, r = 1)
V2 <- area(t = 3, l = 2, b = 3, r = 2)
P4 <- area(t = 3, l = 3, b = 3, r = 3)

# Compose full plot using named areas
ContCoef_GKtau_GKlambda_UncertCoef_Europe <- wrap_plots(
  `P1` = contcoef_Europe + theme(legend.position = "right", plot.margin = unit(c(0,10,10,0), "mm"), legend.key.size = unit(1.2, "cm"), legend.text = element_text(size = 20)),
  `V1` = black_line,
  `P2` = GKtau_Europe + theme(legend.position = "right", plot.margin = unit(c(0,0,10,10), "mm"), legend.key.size = unit(1.2, "cm"), legend.text = element_text(size = 20)),
  `H1` = black_line,
  `X`  = plot_spacer(),  # center of the cross
  `H2` = black_line,
  `P3` = GKlambda_Europe + theme(legend.position = "right", plot.margin = unit(c(10,0,0,0), "mm"), legend.key.size = unit(1.2, "cm"), legend.text = element_text(size = 20)),
  `V2` = black_line,
  `P4` = UncertCoef_Europe + theme(legend.position = "right", plot.margin = unit(c(10,0,0,0), "mm"), legend.key.size = unit(1.2, "cm"), legend.text = element_text(size = 20)),
  design = c(P1, V1, P2, H1, X, H2, P3, V2, P4)
) + 
  plot_layout(
    guides = "collect",
    widths = c(1, 0.01, 1),
    heights = c(1, 0.01, 1)
  )
ggsave(plot = ContCoef_GKtau_GKlambda_UncertCoef_Europe, filename = here("results/Plots/ContCoef_GKtau_GKlambda_UncertCoef_Europe.pdf"), height = 410, width = 450, device = "pdf", units = "mm")

# Position points on border triangles (countries vs income): LIS database ---------------
points_data_Europe <- data.frame(
  lon = c(15, 18.7, 20.5, 16.3, 17, 16.9, 13.5, 9.5, 10.3, 7, 7.6, 6.6, 
          5.6, 6.1, 6, 13.6),
  lat = c(51, 49.5, 46.1, 47, 
          48, 48.8, 48.7, 47.5, 46.7, 46, 47.6, 49.5, 49.5, 50.1, 50.9, 46.4),
  label = c("PlGeCz", "PlSkCz", "RoHuRs", "HuAtSi", 
            "HuSkAt", "CzSkAt", "AtGeCz", "AtGeSw", "AtSwIt", "FrSwIt", "FrSwGe", "LuFrGe", "LuFrBe", 
            "LuGeBe", "NlBeGe", "AtSiIt"),
  wermuthcoef = c(0.141, 0.145, 0.392, 0.381, 
                  0.384, 0.298, 0.144, 0.272, 0.162, 0.147, 0.175, 0.157, 0.175,
                  0.245, 0.216, 0.258),
  CIs_lower = c(0.136, 0.139, 0.384, 0.371, 
                0.375, 0.290, 0.137, 0.265, 0.154, 0.141, 0.170, 0.151, 0.167,
                0.237, 0.211, 0.250),
  CIs_upper = c(0.145, 0.151, 0.401, 0.390,
                0.393, 0.306, 0.151, 0.278, 0.170, 0.154, 0.180, 0.163, 0.183,
                0.252, 0.222, 0.265)
)

wermuthcoef_Europe <- ggplot(data = world) +
  geom_sf(fill = "white", color = "black", linewidth = 0.05) +
  geom_point(data = points_data_Europe, aes(x = lon, y = lat, color = wermuthcoef), size = size) +
  scale_color_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0,1), name = NULL) +
  theme_void() + theme(
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 14)
  ) + 
  coord_sf(xlim = c(5, 32.3), ylim = c(40.5, 57.8), expand = FALSE)
ggsave(plot = wermuthcoef_Europe, filename = here("results/Plots/wermuthcoef_Europe_income.pdf"), height = 200, width = 221, device = "pdf", units = "mm")

wermuthcoef_CIs <- ggplot(points_data_Europe, aes(label, wermuthcoef)) +
  geom_errorbar(aes(ymin = CIs_lower, ymax = CIs_upper), linewidth = 0.8, width = 0.33) +
  geom_point(size = 2) + theme_bw(base_size = 20) + ylim(c(0,0.401)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = TeX("$\\widehat{\\gamma}^*$"), x = "")
ggsave(plot = wermuthcoef_CIs, filename = here("results/Plots/wermuthcoef_Europe_income_CIs.pdf"), height = 20, width = 20, device = "pdf", units = "cm")




